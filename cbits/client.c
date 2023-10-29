/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright (C) 2006-2015, ARM Limited, All Rights Reserved
 *               2020, Intel Labs
 */

/*
 * SSL client demonstration program (with RA-TLS).
 * This program is originally based on an mbedTLS example ssl_client1.c but uses RA-TLS flows (SGX
 * Remote Attestation flows) if RA-TLS library is required by user.
 * Note that this program builds against mbedTLS 3.x.
 */

#include "mbedtls/build_info.h"

#include <assert.h>
#include <ctype.h>
#include <dlfcn.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define mbedtls_fprintf fprintf
#define mbedtls_printf printf

#define MBEDTLS_EXIT_SUCCESS EXIT_SUCCESS
#define MBEDTLS_EXIT_FAILURE EXIT_FAILURE

#include "mbedtls/ctr_drbg.h"
#include "mbedtls/debug.h"
#include "mbedtls/entropy.h"
#include "mbedtls/error.h"
#include "mbedtls/net_sockets.h"
#include "mbedtls/ssl.h"

/* RA-TLS: on client, only need to register ra_tls_verify_callback_der() for cert verification */
int (*ra_tls_verify_callback_der_f)(uint8_t* der_crt, size_t der_crt_size);

/* RA-TLS: if specified in command-line options, use our own callback to verify SGX measurements */
void (*ra_tls_set_measurement_callback_f)(int (*f_cb)(const char* mrenclave, const char* mrsigner,
                                          const char* isv_prod_id, const char* isv_svn));

#define SERVER_PORT "4433"
#define SERVER_NAME "localhost"
#define GET_REQUEST "GET / HTTP/1.0\r\n\r\n"

#define DEBUG_LEVEL 0

#define CA_CRT_PATH "ssl/ca.crt"

static void my_debug(void* ctx, int level, const char* file, int line, const char* str) {
    ((void)level);

    mbedtls_fprintf((FILE*)ctx, "%s:%04d: %s\n", file, line, str);
    fflush((FILE*)ctx);
}

static int parse_hex(const char* hex, void* buffer, size_t buffer_size) {
    if (strlen(hex) != buffer_size * 2)
        return -1;

    for (size_t i = 0; i < buffer_size; i++) {
        if (!isxdigit(hex[i * 2]) || !isxdigit(hex[i * 2 + 1]))
            return -1;
        sscanf(hex + i * 2, "%02hhx", &((uint8_t*)buffer)[i]);
    }
    return 0;
}


/* expected SGX measurements in binary form */
static char g_expected_mrenclave[32];
static char g_expected_mrsigner[32];
static char g_expected_isv_prod_id[2];
static char g_expected_isv_svn[2];

static bool g_verify_mrenclave   = false;
static bool g_verify_mrsigner    = false;
static bool g_verify_isv_prod_id = false;
static bool g_verify_isv_svn     = false;

/* RA-TLS: our own callback to verify SGX measurements */
static int my_verify_measurements(const char* mrenclave, const char* mrsigner,
                                  const char* isv_prod_id, const char* isv_svn) {
    assert(mrenclave && mrsigner && isv_prod_id && isv_svn);

    if (g_verify_mrenclave &&
            memcmp(mrenclave, g_expected_mrenclave, sizeof(g_expected_mrenclave)))
        return -1;

    if (g_verify_mrsigner &&
            memcmp(mrsigner, g_expected_mrsigner, sizeof(g_expected_mrsigner)))
        return -1;

    if (g_verify_isv_prod_id &&
            memcmp(isv_prod_id, g_expected_isv_prod_id, sizeof(g_expected_isv_prod_id)))
        return -1;

    if (g_verify_isv_svn &&
            memcmp(isv_svn, g_expected_isv_svn, sizeof(g_expected_isv_svn)))
        return -1;

    return 0;
}

/* RA-TLS: mbedTLS-specific callback to verify the x509 certificate */
static int my_verify_callback(void* data, mbedtls_x509_crt* crt, int depth, uint32_t* flags) {
    (void)data;

    if (depth != 0) {
        /* the cert chain in RA-TLS consists of single self-signed cert, so we expect depth 0 */
        return MBEDTLS_ERR_X509_INVALID_FORMAT;
    }
    if (flags) {
        /* mbedTLS sets flags to signal that the cert is not to be trusted (e.g., it is not
         * correctly signed by a trusted CA; since RA-TLS uses self-signed certs, we don't care
         * what mbedTLS thinks and ignore internal cert verification logic of mbedTLS */
        *flags = 0;
    }
    return ra_tls_verify_callback_der_f(crt->raw.p, crt->raw.len);
}

static bool getenv_client_inside_sgx() {
    char* str = getenv("RA_TLS_CLIENT_INSIDE_SGX");
    if (!str)
        return false;

    return !strcmp(str, "1") || !strcmp(str, "true") || !strcmp(str, "TRUE");
}


void printBuf(char* buf){
  for(int i = 0; i < 40; i++){
    printf("a[%d] - %u   ", i, buf[i]);
  }
  printf("\n");
}

// NOTE: strcmp returns 0 when strings are equal

int setup_ra_tls_send(char *data, size_t length, char *epidordcap, char* response) {

    int final_data_length = sizeof(size_t) + length; //XXX: for now this <= 1024
    char len_prefixed_data[final_data_length];

    int ret;
    size_t len;
    int exit_code = MBEDTLS_EXIT_FAILURE;
    mbedtls_net_context server_fd;
    uint32_t flags;
    unsigned char buf[1024]; // for now fixing the size at 1024 bytes
    const char* pers = "ssl_client1";
    bool in_sgx = getenv_client_inside_sgx();

    char* error;
    void* ra_tls_verify_lib           = NULL;
    ra_tls_verify_callback_der_f      = NULL;
    ra_tls_set_measurement_callback_f = NULL;

    mbedtls_entropy_context entropy;
    mbedtls_ctr_drbg_context ctr_drbg;
    mbedtls_ssl_context ssl;
    mbedtls_ssl_config conf;
    mbedtls_x509_crt cacert;

#if defined(MBEDTLS_DEBUG_C)
    mbedtls_debug_set_threshold(DEBUG_LEVEL);
#endif

    // ABHI: Initialize the RNG and the session data
    // Takes the branch if (!strcmp(argv[1], "epid"))

    mbedtls_net_init(&server_fd);
    mbedtls_ssl_init(&ssl);
    mbedtls_ssl_config_init(&conf);
    mbedtls_ctr_drbg_init(&ctr_drbg);
    mbedtls_x509_crt_init(&cacert);
    mbedtls_entropy_init(&entropy);

    /* if (argc < 2 || */
    /*     (strcmp(argv[1], "native") && strcmp(argv[1], "epid") && strcmp(argv[1], "dcap"))) { */
    /*     mbedtls_printf("USAGE: %s native|epid|dcap [SGX measurements]\n", argv[0]); */
    /*     return 1; */
    /* } */

    if (!strcmp(epidordcap, "epid")) {
        ra_tls_verify_lib = dlopen("libra_tls_verify_epid.so", RTLD_LAZY);
        if (!ra_tls_verify_lib) {
            mbedtls_printf("%s\n", dlerror());
            mbedtls_printf("User requested RA-TLS verification with EPID but cannot find lib\n");
            if (in_sgx) {
                mbedtls_printf("Please make sure that you are using client_epid.manifest\n");
            }
            return 1;
        }
    } else if (!strcmp(epidordcap, "dcap")) {
        if (in_sgx) {
            /*
             * RA-TLS verification with DCAP inside SGX enclave uses dummies instead of real
             * functions from libsgx_urts.so, thus we don't need to load this helper library.
             */
            ra_tls_verify_lib = dlopen("libra_tls_verify_dcap_gramine.so", RTLD_LAZY);
            if (!ra_tls_verify_lib) {
                mbedtls_printf("%s\n", dlerror());
                mbedtls_printf("User requested RA-TLS verification with DCAP inside SGX but cannot find lib\n");
                mbedtls_printf("Please make sure that you are using client_dcap.manifest\n");
                return 1;
            }
        } else {
            void* helper_sgx_urts_lib = dlopen("libsgx_urts.so", RTLD_NOW | RTLD_GLOBAL);
            if (!helper_sgx_urts_lib) {
                mbedtls_printf("%s\n", dlerror());
                mbedtls_printf("User requested RA-TLS verification with DCAP but cannot find helper"
                               " libsgx_urts.so lib\n");
                return 1;
            }

            ra_tls_verify_lib = dlopen("libra_tls_verify_dcap.so", RTLD_LAZY);
            if (!ra_tls_verify_lib) {
                mbedtls_printf("%s\n", dlerror());
                mbedtls_printf("User requested RA-TLS verification with DCAP but cannot find lib\n");
                return 1;
            }
        }
    }

    // ABHI: Populating the ra_tls_verify_callback_der_f, ra_tls_set_measurement_callback_f
    if (ra_tls_verify_lib) {
        ra_tls_verify_callback_der_f = dlsym(ra_tls_verify_lib, "ra_tls_verify_callback_der");
        if ((error = dlerror()) != NULL) {
            mbedtls_printf("%s\n", error);
            return 1;
        }

        ra_tls_set_measurement_callback_f = dlsym(ra_tls_verify_lib, "ra_tls_set_measurement_callback");
        if ((error = dlerror()) != NULL) {
            mbedtls_printf("%s\n", error);
            return 1;
        }
    }

    /* if (argc > 2 && ra_tls_verify_lib) { // this branch not taken */
    /*     if (argc != 6) { */
    /*         mbedtls_printf("USAGE: %s %s <expected mrenclave> <expected mrsigner>" */
    /*                        " <expected isv_prod_id> <expected isv_svn>\n" */
    /*                        "       (first two in hex, last two as decimal; set to 0 to ignore)\n", */
    /*                        argv[0], argv[1]); */
    /*         return 1; */
    /*     } */

    /*     mbedtls_printf("[ using our own SGX-measurement verification callback" */
    /*                    " (via command line options) ]\n"); */

    /*     g_verify_mrenclave   = true; */
    /*     g_verify_mrsigner    = true; */
    /*     g_verify_isv_prod_id = true; */
    /*     g_verify_isv_svn     = true; */

    /*     (*ra_tls_set_measurement_callback_f)(my_verify_measurements); */

    /*     if (!strcmp(argv[2], "0")) { */
    /*         mbedtls_printf("  - ignoring MRENCLAVE\n"); */
    /*         g_verify_mrenclave = false; */
    /*     } else if (parse_hex(argv[2], g_expected_mrenclave, sizeof(g_expected_mrenclave)) < 0) { */
    /*         mbedtls_printf("Cannot parse MRENCLAVE!\n"); */
    /*         return 1; */
    /*     } */

    /*     if (!strcmp(argv[3], "0")) { */
    /*         mbedtls_printf("  - ignoring MRSIGNER\n"); */
    /*         g_verify_mrsigner = false; */
    /*     } else if (parse_hex(argv[3], g_expected_mrsigner, sizeof(g_expected_mrsigner)) < 0) { */
    /*         mbedtls_printf("Cannot parse MRSIGNER!\n"); */
    /*         return 1; */
    /*     } */

    /*     if (!strcmp(argv[4], "0")) { */
    /*         mbedtls_printf("  - ignoring ISV_PROD_ID\n"); */
    /*         g_verify_isv_prod_id = false; */
    /*     } else { */
    /*         errno = 0; */
    /*         uint16_t isv_prod_id = (uint16_t)strtoul(argv[4], NULL, 10); */
    /*         if (errno) { */
    /*             mbedtls_printf("Cannot parse ISV_PROD_ID!\n"); */
    /*             return 1; */
    /*         } */
    /*         memcpy(g_expected_isv_prod_id, &isv_prod_id, sizeof(isv_prod_id)); */
    /*     } */

    /*     if (!strcmp(argv[5], "0")) { */
    /*         mbedtls_printf("  - ignoring ISV_SVN\n"); */
    /*         g_verify_isv_svn = false; */
    /*     } else { */
    /*         errno = 0; */
    /*         uint16_t isv_svn = (uint16_t)strtoul(argv[5], NULL, 10); */
    /*         if (errno) { */
    /*             mbedtls_printf("Cannot parse ISV_SVN\n"); */
    /*             return 1; */
    /*         } */
    /*         memcpy(g_expected_isv_svn, &isv_svn, sizeof(isv_svn)); */
    /*     } */
    /* } */
    if (ra_tls_verify_lib) { // this branch taken
        mbedtls_printf("[ using default SGX-measurement verification callback"
                       " (via RA_TLS_* environment variables) ]\n");
        (*ra_tls_set_measurement_callback_f)(NULL); /* just to test RA-TLS code */
    } else {
        mbedtls_printf("[ using normal TLS flows ]\n");
    }

    mbedtls_printf("\n  . Seeding the random number generator...");
    fflush(stdout);

    ret = mbedtls_ctr_drbg_seed(&ctr_drbg, mbedtls_entropy_func, &entropy,
                                (const unsigned char*)pers, strlen(pers));
    if (ret != 0) {
        mbedtls_printf(" failed\n  ! mbedtls_ctr_drbg_seed returned %d\n", ret);
        goto exit;
    }

    mbedtls_printf(" ok\n");

    // ABHI: Start the connection

    mbedtls_printf("  . Connecting to tcp/%s/%s...", SERVER_NAME, SERVER_PORT);
    fflush(stdout);

    ret = mbedtls_net_connect(&server_fd, SERVER_NAME, SERVER_PORT, MBEDTLS_NET_PROTO_TCP);
    if (ret != 0) {
        mbedtls_printf(" failed\n  ! mbedtls_net_connect returned %d\n\n", ret);
        goto exit;
    }

    mbedtls_printf(" ok\n");

    // ABHI: Setup stuff

    mbedtls_printf("  . Setting up the SSL/TLS structure...");
    fflush(stdout);

    ret = mbedtls_ssl_config_defaults(&conf, MBEDTLS_SSL_IS_CLIENT, MBEDTLS_SSL_TRANSPORT_STREAM,
                                      MBEDTLS_SSL_PRESET_DEFAULT);
    if (ret != 0) {
        mbedtls_printf(" failed\n  ! mbedtls_ssl_config_defaults returned %d\n\n", ret);
        goto exit;
    }

    mbedtls_printf(" ok\n");

    // ABHI: Initializing the loading the CA root certificate

    mbedtls_printf("  . Loading the CA root certificate ...");
    fflush(stdout);

    ret = mbedtls_x509_crt_parse_file(&cacert, CA_CRT_PATH);
    if (ret < 0) {
        mbedtls_printf( " failed\n  !  mbedtls_x509_crt_parse_file returned -0x%x\n\n", -ret );
        goto exit;
    }

    mbedtls_ssl_conf_authmode(&conf, MBEDTLS_SSL_VERIFY_OPTIONAL);
    mbedtls_ssl_conf_ca_chain(&conf, &cacert, NULL);
    mbedtls_printf(" ok\n");

    // XXX: ABHI: IMP installing verification callback here
    if (ra_tls_verify_lib) {
        /* use RA-TLS verification callback; this will overwrite CA chain set up above */
        mbedtls_printf("  . Installing RA-TLS callback ...");
        mbedtls_ssl_conf_verify(&conf, &my_verify_callback, NULL);
        mbedtls_printf(" ok\n");
    }

    mbedtls_ssl_conf_rng(&conf, mbedtls_ctr_drbg_random, &ctr_drbg);
    mbedtls_ssl_conf_dbg(&conf, my_debug, stdout);

    ret = mbedtls_ssl_setup(&ssl, &conf);
    if (ret != 0) {
        mbedtls_printf(" failed\n  ! mbedtls_ssl_setup returned %d\n\n", ret);
        goto exit;
    }

    ret = mbedtls_ssl_set_hostname(&ssl, SERVER_NAME);
    if (ret != 0) {
        mbedtls_printf(" failed\n  ! mbedtls_ssl_set_hostname returned %d\n\n", ret);
        goto exit;
    }

    mbedtls_ssl_set_bio(&ssl, &server_fd, mbedtls_net_send, mbedtls_net_recv, NULL);

    // ABHI : Handshake

    mbedtls_printf("  . Performing the SSL/TLS handshake...");
    fflush(stdout);

    while ((ret = mbedtls_ssl_handshake(&ssl)) != 0) {
        if (ret != MBEDTLS_ERR_SSL_WANT_READ && ret != MBEDTLS_ERR_SSL_WANT_WRITE) {
            mbedtls_printf(" failed\n  ! mbedtls_ssl_handshake returned -0x%x\n\n", -ret);
            goto exit;
        }
    }

    mbedtls_printf(" ok\n");

    // ABHI: Server certificate verification

    mbedtls_printf("  . Verifying peer X.509 certificate...");

    flags = mbedtls_ssl_get_verify_result(&ssl);
    if (flags != 0) {
        char vrfy_buf[512];
        mbedtls_printf(" failed\n");
        mbedtls_x509_crt_verify_info(vrfy_buf, sizeof(vrfy_buf), "  ! ", flags);
        mbedtls_printf("%s\n", vrfy_buf);

        /* verification failed for whatever reason, fail loudly */
        goto exit;
    } else {
        mbedtls_printf(" ok\n");
    }

    // ABHI: Write the GET request

    mbedtls_printf("  > Write to server:");
    fflush(stdout);

    //len = sprintf((char*)buf, GET_REQUEST); // XXX: ABHI: This is where we send the data
    // invariant- sizeof(len_prefixed_data) = sizeof(size_t) + sizeof(data) or length
    //memcpy(len_prefixed_data, &length, sizeof(size_t));
    size_t data_len = length;
    for (int i = 7; i >= 0; i--) {
      len_prefixed_data[i] = (char)(data_len & 0xFF);
      data_len >>= sizeof(size_t);
    }


    memcpy(len_prefixed_data + sizeof(size_t), data, length);


    /* if(final_data_length > sizeof(buf)) // bound check */
    /*   return 1; */
    memcpy((char*)buf, len_prefixed_data, final_data_length);
    len = final_data_length;

    for(int i = 0; i <final_data_length; i++){
      printf("buf[%d] - %u\n", i, buf[i]);
    }


    while ((ret = mbedtls_ssl_write(&ssl, buf, len)) <= 0) {
        if (ret != MBEDTLS_ERR_SSL_WANT_READ && ret != MBEDTLS_ERR_SSL_WANT_WRITE) {
            mbedtls_printf(" failed\n  ! mbedtls_ssl_write returned %d\n\n", ret);
            goto exit;
        }
    }

    len = ret;
    mbedtls_printf(" %lu bytes written\n\n%s", len, (char*)buf);

    mbedtls_printf("  < Read from server:");
    fflush(stdout);

    do {
        len = sizeof(buf) - 1;
        memset(buf, 0, sizeof(buf));
        ret = mbedtls_ssl_read(&ssl, buf, len);

        if (ret == MBEDTLS_ERR_SSL_WANT_READ || ret == MBEDTLS_ERR_SSL_WANT_WRITE)
            continue;

        if (ret == MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY)
            break;

        if (ret < 0) {
            mbedtls_printf("failed\n  ! mbedtls_ssl_read returned %d\n\n", ret);
            break;
        }

        if (ret == 0) {
            mbedtls_printf("\n\nEOF\n\n");
            break;
        }

        len = ret;
        mbedtls_printf(" %lu bytes read\n\n%s", len, (char*)buf);
        memcpy(response, buf, 1024);// length of both arrays static at 1024
    } while (1);

    printBuf(response);

    mbedtls_ssl_close_notify(&ssl);
    exit_code = MBEDTLS_EXIT_SUCCESS;
exit:
#ifdef MBEDTLS_ERROR_C
    if (exit_code != MBEDTLS_EXIT_SUCCESS) {
        char error_buf[100];
        mbedtls_strerror(ret, error_buf, sizeof(error_buf));
        mbedtls_printf("Last error was: %d - %s\n\n", ret, error_buf);
    }
#endif

    if (ra_tls_verify_lib)
        dlclose(ra_tls_verify_lib);

    mbedtls_net_free(&server_fd);

    mbedtls_x509_crt_free(&cacert);
    mbedtls_ssl_free(&ssl);
    mbedtls_ssl_config_free(&conf);
    mbedtls_ctr_drbg_free(&ctr_drbg);
    mbedtls_entropy_free(&entropy);

    return exit_code;
}

/*
 *
 #include <stdio.h>

 #define GET_REQUEST "GET / HTTP/1.0\n\n"

 int main() {

 unsigned char buf[1024];
 size_t len = sprintf((char*)buf, GET_REQUEST);
 printf("\n value of a_static: %s", buf);

 return 0;
 }


 */

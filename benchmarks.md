#### Response Time for password retrieval (involves file read as well)

Time is measure in seconds 

Note the non-SGX Haskell file is unencrypted

Average numbers for non-SGX Haskell + encryption ~ 0.0006-0.0008; encryption (gpg) adds 0.0002

HasTEE + SGX |  Haskell (no SGX) | C (with SGX)
------------------------------------------------
0.069940556  | 0.000294702       |
0.068725749  | 0.000281502       |
0.067006642  | 0.000269602       |
0.066853754  | 0.000269502       |
0.065263212  | 0.000263302       |
0.067077723  | 0.000273002       |
0.065565735  | 0.000270201       |
0.066263339  | 0.000267401       |
0.069965863  | 0.000278902       |
0.067200845  | 0.000266402       |
0.066725642  | 0.000261902       |
0.066660041  | 0.000669205       |
0.064793828  | 0.000265201       |
0.067570645  | 0.000270102       |
0.066741938  | 0.000346202       |
0.066609036  | 0.000265802       |
0.069043651  | 0.000265202       |
0.064523418  | 0.000271402       |
0.068659852  | 0.000312402       |
0.068592748  | 0.000316202       |
0.065454141  | 0.000267502       |
0.068226754  | 0.000277202       |
0.068951357  | 0.000265802       |


##### Memory usage

ps aux | grep gramine
syrupy.py -p <<pid>>

pidstat -h -r -u -v -p 2070798  1
sample every 1 second for pid 2070798

pmap <<pid>> | tail -n 1
287920 KB - 287.92 MB
287924 KB - 287.92 MB

ps
--
RSS - RSS is Resident Set Size. This is the size of memory that a process has currently used to load all of its pages. At first glance, it may seem like the RSS number is the real amount of physical memory that a system process is using. However, shared libraries are counted for each process, making the reported amount of physical memory usage less accurate.

Vsize - VSZ is Virtual Memory Size. This is the size of memory that Linux has given to a process, but it doesn’t necessarily mean that the process is using all of that memory. The VSZ size you see has taken all of these pages into consideration, but it doesn’t mean they’ve been loaded into physical memory.

Wallet app
----------
              RSS        Vsize
at rest     19132KB     287920KB
peak        20796KB     290032KB

No disk swapping
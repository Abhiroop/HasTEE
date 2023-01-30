{-# LANGUAGE CPP #-}
module FedLearnServer where

import FedLearnUtils

#ifdef ENCLAVE
import Server
#else
import Client
#endif

import Crypto.Paillier


getPubKey :: Server PubKey
getPubKey = undefined

type Id = Int
type IterN = Int

-- this function should update the inner state that
-- stores the updated_weights variable
{-
    self.updated_weights = (1 / self.worker_num)
                         * np.sum(self.weights_dict[iter_n], axis=0)
    if self.secure:
      self.updated_weights = self.re_encrypt(self.updated_weights)
-}
aggregateModel :: Id -> IterN -> CipherText -> Server ()
aggregateModel = undefined

type Accuracy = Double
type Loss = Double
type DataSet = [[Double]] -- some dummy type

-- parse dataset; get x,y and call internal validate
-- calculate acc and loss using self.updated_weights
-- see NOTE 1
validate :: DataSet -> Server (Accuracy, Loss)
validate = undefined

finish :: Server ()
finish = return () -- a no op to begin with; not splitting
                   -- the application into two parts now

-- NOTE 1
-- The validate function actually uses the weights to
-- calculate the accuracy and loss. It is important to
-- see if there is any information leak by leaking the
-- accuracy and loss values

-- http://acmp.ru/index.asp?main=task&id_task=176

main = let
         n = 5
         k = 2
       in
         print (rbs (n*2) 0 1 k)

rbs 0 0 0 _ = 1
rbs 0 _ _ _ = 0
rbs _ k _ _ | k < 0 = 0

rbs n k 0 max_k
  | k < max_k - 1 = (rbs (n-1) (k-1) 0 max_k) + (rbs (n-1) (k+1) 0 max_k)
  | k < max_k     = rbs (n-1) (k-1) 0 max_k
  | k == max_k    = 0

rbs n k 1 max_k
  | k < max_k  = (rbs (n-1) (k-1) 1 max_k) + (rbs (n-1) (k+1) 1 max_k)
  | k == max_k = (rbs (n-1) (k-1) 0 max_k) + (rbs (n-1) (k-1) 1 max_k)

f q x = 1 - (x/q)

fs q = map (f q) [1..50]

pHits 0 [] = 1
pHits _ [] = 0
pHits 0 (p:ps) = (1-p) * (pHits 0 ps)
pHits hits (p:ps) = p * (pHits (hits-1) ps) + (1-p) * (pHits hits ps)

pH20Q51 = pHits 20 (fs 51)


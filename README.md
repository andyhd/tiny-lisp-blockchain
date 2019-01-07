# tiny-lisp-blockchain
Common Lisp implementation of https://github.com/lucrussell/tiny-blockchain

Run `docker-compose up` and you should be able to browse to http://localhost:5001/, http://localhost:5002/ and http://localhost:5003

Fetch the JSON representation of the blockchain from a node at http://localhost:5001/blocks

POST transactions to http://localhost:5001/transaction with a form-encoded body like the following:
```
from={address}&to={address}&amount={amount}
```

Mine a new block and add it to the blockchain by visiting http://localhost:5001/mine

Consensus algorithm currently fetches the blockchains held by peer nodes and selects the longest

Proof of work algorithm is not implemented yet

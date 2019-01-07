(defpackage tiny-lisp-blockchain.blockchain
  (:use :cl)
  (:import-from :ironclad
                :ascii-string-to-byte-array
                :byte-array-to-hex-string
                :digest-sequence)
  (:import-from :json
                :decode-json-from-string
                :encode-json-to-string)
  (:import-from :local-time
                :format-timestring
                :now
                :parse-timestring)
  (:import-from :tiny-lisp-blockchain.config
                :config)
  (:import-from :uuid
                :make-v4-uuid))

(in-package tiny-lisp-blockchain.blockchain)

(defclass bloc ()
  ((index :accessor index
          :initform nil
          :initarg :index)
   (timestamp :accessor timestamp
              :initform nil
              :initarg :timestamp)
   (data :accessor data
         :initform nil
         :initarg :data)
   (previous-hash :accessor previous-hash
                  :initform nil
                  :initarg :previous-hash)
   (hash :accessor hash
         :initform nil
         :initarg :hash)))

(defmethod initialize-instance :after ((this bloc) &key)
  (with-slots (hash) this
    (setf hash (calculate-hash this))))

(defmethod get-proof-of-work ((this bloc))
  (with-slots (data) this
    (cdr (assoc "proof-of-work" data :test #'string=))))

(defun sha-256 (str)
  (byte-array-to-hex-string
    (digest-sequence :sha256 (ascii-string-to-byte-array str))))

(defmethod calculate-hash ((this bloc))
  (with-slots (index timestamp data previous-hash) this
    (let ((s (format nil "~A~A~A~A"
                     index
                     (format-timestring nil timestamp)
                     (encode-json-to-string data)
                     previous-hash)))
      (sha-256 s))))

(defun make-blockchain ()
  (list (make-instance 'bloc
                       :index 0
                       :timestamp (now)
                       :data (list
                               '("proof-of-work" . 9)
                               '("transactions" . nil)
                       )
                       :previous-hash "0")))

(defmethod next-block ((this bloc) &key (data nil))
  (with-slots (index timestamp hash) this
    (let ((next-index (1+ index)))
      (make-instance 'bloc
                     :index next-index
                     :timestamp (now)
                     :data data
                     :previous-hash hash))))

(defmethod to-json ((this bloc))
  (with-slots (index timestamp data previous-hash hash) this
    `(("index" . ,index)
      ("timestamp" . ,(format-timestring nil timestamp))
      ("data" . ,data)
      ("previous-hash" . ,previous-hash)
      ("hash" . ,hash))))

(defun block-from-json (data)
  (make-instance 'bloc
                 :index (cdr (assoc :index data))
                 :timestamp (parse-timestring (cdr (assoc :timestamp data)))
                 :data (cdr (assoc :data data))
                 :previous-hash (cdr (assoc :previous-hash data))
                 :hash (cdr (assoc :hash data))))

(defmethod is-valid ((this bloc) (previous bloc))
  (cond
    ((not (equal (1+ (index previous)) (index this)))
       (print "invalid index"))
    ((not (equal (hash previous) (previous-hash this)))
       (print "invalid previous-hash"))
    ((not (equalp (calculate-hash this) (hash this)))
       (format t "invalid hash: ~A ~A" (calculate-hash this) (hash this)))
    (t t)))

(defclass node ()
  ((miner-address :accessor miner-address
                  :initform nil)
   (blockchain :accessor blockchain
               :initform nil)
   (peers :accessor peers
          :initform nil)
   (transactions :accessor transactions
                 :initform nil)))

(defmethod initialize-instance :after ((this node) &key)
  (setf (miner-address this) (format nil "~A" (make-v4-uuid)))
  (setf (blockchain this) (make-blockchain))
  (setf (peers this) (config 'tiny-lisp-blockchain.config::peers)))

(defmethod transaction ((this node) data)
  (setf (transactions this) (append (transactions this) (list data))))

(defmethod blockchain-to-json ((this node))
  (encode-json-to-string (mapcar #'to-json (blockchain this))))

(defmethod mine ((this node))

  ; get consensus on blockchain state
  (consensus this)

  (let ((last-block (car (last (blockchain this))))
        (last-proof nil)
        (proof nil)
        (mined-block nil))

    (setf last-proof (get-proof-of-work last-block))
    (setf proof (proof-of-work last-proof))

    ; reward the miner by adding a transaction
    (nconc (transactions this)
           (list `(("from" . "network")
                   ("to" . ,(miner-address this))
                   ("amount" . 1))))

    ; add the proof and list of transactions to the block
    (setf mined-block
          (next-block last-block
                      :data `(("proof-of-work" . ,proof)
                              ("transactions" . ,(copy-tree (transactions this))))))

    ; reset transaction list
    (setf (transactions this) nil)

    ; add the new block
    (nconc (blockchain this) (list mined-block))
    (format t "Added block #~A to the blockchain!" (index mined-block))

    (encode-json-to-string (to-json mined-block))))

(defmethod consensus ((this node))
  ; get the blocks from other nodes
  (let* ((chains (all-chains this))
         (longest (first (sort chains '> :key #'list-length))))
    ; if our chain isn't longest, then we store the longest chain
    (when (> (list-length longest) (list-length (blockchain this)))
      (format t "Found longer blockchain: ~A > ~A~%" (list-length longest) (list-length (blockchain this)))
      (setf (blockchain this) longest))))

(defmethod all-chains ((this node))
  ; get the blockchains of every other node
  (format t "Searching peer nodes: ~A~%" (peers this))
  (mapcar (lambda (peer)
            (multiple-value-bind (body status)
                (dex:get (format nil "http://~A/blocks" peer))
              (if (eql status 200)
                (mapcar #'block-from-json (decode-json-from-string body))
                (format t "Unable to retrieve blockchain from peer ~A" peer))))
          (peers this)))

(defun proof-of-work (last-proof)
  ; not implemented yet
  (declare (ignore last-proof))
  9)

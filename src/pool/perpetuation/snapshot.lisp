(in-package :upanishad.pool)

(defun get-snapshot-objects (pool type)
  "pool からシリアライズするオブジェクトを取得する。"
  (cond ((eq type :object) (root-objects pool))
        ((eq type :index) (index-objects pool))
        ((eq type :memes) (memes pool))
        ((eq type :indexes) (indexes pool))))

(defun snapshot-objects (pool snapshot type)
  "type で指定された Objects をシリアライズする。
シリアライズしたものを snapshot に書き込む。"
  (assert (snapshot-type-p type))
  (let ((objects (get-snapshot-objects pool type))
        (serializer (serializer pool)))
    (with-open-file (out snapshot :direction         :output
                                  :if-does-not-exist :create
                                  :if-exists         :supersede)
      (funcall serializer objects out (serialization-state pool)))))

(defmethod snapshot ((pool pool))
  (let ((timetag (timetag))
        (transaction-log (transaction-log pool))
        (snapshot-object  (snapshot-pathnames pool :object))
        (snapshot-index   (snapshot-pathnames pool :index))
        (snapshot-memes   (snapshot-pathnames pool :memes))
        (snapshot-indexes (snapshot-pathnames pool :indexes)))
    (close-open-streams pool)
    ;; backup snapshot
    (snapshot-copy-snapshot-file pool snapshot-object  :object  timetag)
    (snapshot-copy-snapshot-file pool snapshot-index   :index   timetag)
    (snapshot-copy-snapshot-file pool snapshot-memes   :memes   timetag)
    (snapshot-copy-snapshot-file pool snapshot-indexes :indexes timetag)
    ;; snapshot
    (snapshot-objects pool snapshot-object  :object)
    (snapshot-objects pool snapshot-index   :index)
    (snapshot-objects pool snapshot-memes   :memes)
    (snapshot-objects pool snapshot-indexes :indexes)
    ;; transaction log
    (snapshot-transaction-log pool transaction-log timetag)
    (when (fad:file-exists-p transaction-log)
      (delete-file transaction-log))
    pool))

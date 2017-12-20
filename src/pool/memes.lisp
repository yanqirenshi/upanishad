(in-package :upanishad.pool)

(defgeneric tx-add-memes (pool memes)
  (:method ((pool pool) (memes up.memes:memes))
    (let ((ht (memes pool))
          (key (up.memes:meme-class memes)))
      (when (gethash key ht)
        (error "このキーの memes は既に存在します。"))
      (setf (gethash key ht) memes)
      memes))
  (:method ((pool pool) (class symbol))
    (let ((memes (make-instance 'up.memes:memes
                                :meme-class class)))
      (tx-add-memes pool memes)))
  (:documentation "プールの memes スロットに memes を追加する。"))

(defgeneric tx-remove-memes (pool memes)
  (:method ((pool pool) (memes up.memes:memes))
    (tx-remove-memes pool (up.memes:meme-class memes)))
  (:method ((pool pool) (class symbol))
    (let ((ht (memes pool))
          (key class))
      (when (gethash key ht)
        (remhash key ht))))
  (:documentation "プールの memes スロットから memes を削除する。"))

(defun %get-memes (pool class)
  (let ((ht (memes pool))
        (key class))
    (gethash key ht)))

(defgeneric get-memes (pool &key class ensure)
  (:method ((pool pool) &key class (ensure t))
    (let ((memes (%get-memes pool class)))
      (or memes
          (when ensure
            (tx-add-memes pool class)))))
  (:documentation "プールの memes スロットから memes クラスのインスタンスを取得する。"))

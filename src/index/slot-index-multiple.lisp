(in-package :upanishad.index)

;;;
;;; util
;;;
(defun ensure-%id->object (value->objects value)
  "%id-object を返す。
slot-index-multiple.value-objects に %id-object(連想配列)が存在しない場合
連想配列を作成してそれを返す。"
  (or (gethash value value->objects)
      (setf (gethash value value->objects)
            (make-hash-table :test 'equalp))))

(defun slot-index-multiple-contexts (index meme)
  "slot-index-multiple の処理に必要な値を返す。
各オペレータの let が長くなる。それを短かくするための関数。"
  (multiple-value-bind (class slot)
      (get-index-key index)
    (assert-class class meme)
    (let* ((%id (%id meme))
           (value (slot-value meme slot))
           (value->objects (value->objects index)))
      (values %id
              value
              ;; TODO: 
              (%id->value index)
              (ensure-%id->object value->objects value)
              value->objects))))

;;;
;;; add-object
;;;
(defun add-object-add-multi (meme
                             value
                             %id
                             %id->object
                             %id->value)
  "meme を index に追加するオペレータ。
add/change の両方からコールするので汎用的に別出し。"
  (assert (or meme value %id %id->object %id->value))
  (setf (gethash %id %id->value) value)
  (setf (gethash %id %id->object) meme))

(defmethod add-object ((index slot-index-multiple) meme)
  "meme を index に追加するオペレータ。
処理は実質 `add-object-add-multi` で行っている。"
  (multiple-value-bind (%id value %id->value %id->object)
      (slot-index-multiple-contexts index meme)
    (if (gethash %id %id->value)
        (unless (equalp (gethash %id %id->value) value)
          (error "すでに他の値で登録されています。"))
        (add-object-add-multi meme value %id
                              %id->object %id->value)))
  index)

;;;
;;; add-objects
;;;
(defmethod add-objects ((index slot-index-multiple) objects)
  (dolist (object objects)
    (add-object index object)))

;;;
;;; remove-meme
;;;
(defun add-object-remove-multi (value
                                %id
                                %id->object
                                %id->value
                                value->objects)
  "meme を index から削除するためのオペレータ。
add/change の両方からコールするので汎用的に別出し。"
  (assert (or value %id %id->object %id->value value->objects))

  (remhash %id %id->value)
  (remhash %id %id->object)
  (when (= (hash-table-count %id->object) 0)
    (remhash value value->objects)))


(defmethod remove-object ((index slot-index-multiple) meme)
  "meme を index から削除するためのオペレータ。
処理は実質 `add-object-remove-multi` で行っている。"
  (multiple-value-bind (%id value
                        %id->value %id->object value->objects)
      (slot-index-multiple-contexts index meme)
    (add-object-remove-multi value
                             %id
                             %id->object
                             %id->value
                             value->objects)))

;;;
;;; change-meme
;;;
(defmethod change-object ((index slot-index-multiple) meme)
  (multiple-value-bind (%id value
                        %id->value %id->object value->objects)
      (slot-index-multiple-contexts index meme)

    (let ((old-value (gethash %id %id->value)))
      (add-object-remove-multi old-value
                               %id
                               %id->object
                               (gethash old-value value->objects)
                               value->objects)

      (add-object-add-multi meme value %id
                            %id->object %id->value)

      index)))

;;;
;;; 
;;;
(defmethod get-at-value ((index slot-index-multiple) value)
  (let ((%id->object (gethash value (value->objects value))))
    (alexandria:hash-table-values %id->object)))

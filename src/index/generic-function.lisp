(in-package :upanishad.index)

(defgeneric get-index-key (index)
  (:documentation "インデックスの情報を返す。"))

(defgeneric get-at-value (index value)
  (:documentation "インデックスを値で検索し、その値に紐付くオブジェクトを返す。"))

(defgeneric add-object (index object)
  (:documentation "インデックスにオブジェクトを追加します。"))

(defgeneric add-objects (index objects)
  (:documentation "インデックスに複数のオブジェクトを追加します。"))

(defgeneric remove-object (index object)
  (:documentation "インデックスからオブジェクトを削除します。"))

(defgeneric change-object (index object)
  (:documentation ""))

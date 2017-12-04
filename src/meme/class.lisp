(in-package :upanishad.meme)

;;;;;
;;;;; Brahman
;;;;;
(defclass brahman () ()
  (:documentation "思想的/象徴的なクラス。今んところ意味はないけぇ。"))

;;;;;
;;;;; Atman
;;;;;
(defclass atman (brahman)
  ((%id :documentation "Return an external, unique, immutable identifier for object (typically an integer)"
        :reader %id
        :initarg :%id
        :initform -1))
  (:documentation "Superclass for objects with an id"))

;;;;;
;;;;; Meme
;;;;;
(defclass meme (atman)
  ()
  (:documentation "このクラスも思想的/象徴的なクラスです。まぁぶっちゃけ不要なんですけど。"))

;;;;;
;;;;; Blob
;;;;;
(defclass blob (meme)
  ((name :documentation "Return the descriptive name of blob. Set the descriptive name of blob."
         :accessor name
         :initarg :name
         :initform "untitled")
   (size :documentation "Return the size of blob in bytes. Set the mime-type string of blob."
         :reader size
         :initarg :size
         :initform -1)
   (mime-type :documentation "Return the mime-type of blob as a string. Set the keywords list of blob"
              :accessor mime-type
              :initarg :mime-type
              :initform "application/octet-stream")
   (keywords :documentation "Return the list of keywords associated with blob"
             :accessor keywords
             :initarg :keywords
             :initform '()))
  (:documentation "A blob is a file-like collection of bytes with related metadata"))

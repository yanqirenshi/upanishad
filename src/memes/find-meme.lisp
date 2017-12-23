(in-package :upanishad.memes)

(defun find-meme (memes &key slot value)
  "list から slot = value の meme のリストを返します。"
  (first (find-if #'(lambda (meme)
                      (equalp (slot-value meme slot)
                              value))
                  memes)))

(in-package :upanishad)

(defgeneric tx-add-meme-to-index (pool slot meme)
  (:method ((pool pool) (slot symbol) (meme meme))
    (let ((index (get-index pool (class-name (class-of meme)) slot)))
      (unless index (error "index not found"))
      (up.index:add-object index meme))))

(defgeneric tx-remove-meme-from-index (pool slot meme)
  (:method ((pool pool) (slot symbol) (meme meme))
    (let ((index (get-index pool (type-of meme) slot)))
      (when index
        (up.index:remove-object index meme)))))

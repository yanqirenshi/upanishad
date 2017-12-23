(in-package :upanishad.memes)

(defun get-meme (memes &key %id slot value)
  "プライマリキーで meme を取得します。"
  (assert memes)
  (cond (%id (%get-meme memes %id))
        (slot (warn "get-meme での slot value の利用は非推奨です。そのうち勝手に廃止します。")
              (find-meme memes :slot slot :value value))
        (t nil)))

(defun %get-meme (memes %id)
  "プライマリキーで meme を取得します。"
  (get-at-value (%id-index memes) %id))

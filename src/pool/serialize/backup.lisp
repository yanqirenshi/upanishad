(in-package :upanishad.pool)

(defmethod backup ((pool pool) &key directory)
  (let* ((timetag (timetag))
         (transaction-log (transaction-log pool))
         (transaction-log-backup (transaction-log-backup-file pool (or directory transaction-log) timetag))
         ;; object
         (snapshot-object (snapshot-pathnames pool :object))
         (snapshot-object-backup (make-snapshot-backup-pathname pool (or directory snapshot-object) :object timetag))
         ;; index
         (snapshot-index (snapshot-pathnames pool :index))
         (snapshot-index-backup (make-snapshot-backup-pathname pool (or directory snapshot-index) :index timetag))
         ;; memes
         (snapshot-memes (snapshot-pathnames pool :memes))
         (snapshot-memes-backup (make-snapshot-backup-pathname pool (or directory snapshot-index) :memes timetag))
         ;; indexes
         (snapshot-indexes (snapshot-pathnames pool :indexes))
         (snapshot-indexes-backup (make-snapshot-backup-pathname pool (or directory snapshot-index) :indexes timetag)))
    (close-open-streams pool)
    (backup-transaction-log transaction-log transaction-log-backup)
    (backup-snapshot snapshot-object  snapshot-object-backup)
    (backup-snapshot snapshot-index   snapshot-index-backup)
    (backup-snapshot snapshot-memes   snapshot-memes-backup)
    (backup-snapshot snapshot-indexes snapshot-indexes-backup)
    timetag))

<usage>
    <section class="section">
        <div class="container">
            <h1 class="title is-2">Usage</h1>
            <!-- <p class="subtitle"></p> -->
            <p>
                データを保管する(snapshotと言う)ためのディレクトリを作成したら。。。
                <pre>
                    <code>
cd ~/
mkdir ~/up
                    </code>
                </pre>
            </p>

            <p>
                あとは lisp でこんな感じ。
            </p>

            <pre>
                <code>
;; pool の開始 ※pool は db に脳内変換すると違和感がなくなります。
(defvar *pool* (make-pool "~/up/"))
(execute-transaction (tx-create-id-counter *pool*))
    ：
  ※いろいろな処理。 準備中
    ：
;; snapshot の実行。  ※snapshot は commit に脳内変換。
(snapshot *test-system*)
    ：
  ※いろいろな処理。 準備中
    ：
;; restore の実行  ※restore は rollback に脳内変換。
(restore *bank-system*)

;; pool の停止
(close-open-streams *test-system*)
                </code>
            </pre>
        </div>
    </section>

    <style>
     usage > .section {
         padding-top: 11px;
         padding-bottom: 11px;
     }
    </style>
</usage>

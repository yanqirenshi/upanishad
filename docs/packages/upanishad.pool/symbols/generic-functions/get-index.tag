<get-index>
    <up-header title="Generic function: GET-INDEX">
        <h2 class="subtitle">
            class の slot-index を取得する。
        </h2>
    </up-header>

    <up-section title="Statement">
        <syntax-operator operator="get-index"
                         arguments="pool class slot"
                         results="slot-index"></syntax-operator>
    </up-section>

    <up-section title="Description">
        <p>
            CLASS の SLOT-INDEX を取得する。
        </p>
        <p>
            スロットが %id の場合は MEMES から取得する。<br/>
            それ以外のスロットの場合は POOL の INDEXES スロットから取得する。
        </p>
    </up-section>

    <up-section title="Arguments">
        <arguments-list args={this.args}></arguments-list>
    </up-section>

    <up-section title="Values">
    </up-section>

    <up-section title="Conditions">
        なし
    </up-section>

    <up-section title="See also">
    </up-section>

    <script>
     this.args = [
         {
             name: 'pool',
             type: 'Class: POOL',
             description: ''
         }
         , {
             name: 'class',
             type: 'CLASS',
             description: ''
         }
         , {
             name: 'slot',
             type: 'Symbol',
             description: ''
         }
     ];
    </script>
</get-index>

<tx-add-memes>
    <up-header title="Generic function: TX-ADD-MEMES">
        <h2 class="subtitle">
        </h2>
    </up-header>

    <up-section title="Statement">
        <syntax-operator operator="get-memes"
                         arguments="pool &key class ensure"
                         results="memes"></syntax-operator>
    </up-section>

    <up-section title="Description">
        <p>
        POOL に memes を追加します。<br/>
        memes は引数で指定します。<br/>
        </p>

        <p>
        memes は POOL の memes スロットに追加されます。<br/>
        POOL の memes スロットは連想配列でキーは memes が管理する class です。
        </p>
    </up-section>

    <up-section title="Arguments">
        <arguments-list args={this.args}></arguments-list>
    </up-section>

    <up-section title="Values">
        <p>
            POOL に追加した memes を返します。
        </p>
    </up-section>

    <up-section title="Conditions">
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
             name: 'memes',
             type: 'Class: MEMES',
             description: ''
         }
     ];
    </script>
</tx-add-memes>

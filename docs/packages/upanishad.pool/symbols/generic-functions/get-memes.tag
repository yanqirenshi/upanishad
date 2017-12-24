<get-memes>
    <up-header title="Generic function: GET-MEMES">
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
            POOL から class を管理する memes を返します。
        </p>

        <p>
            ensure が t の場合 memes が存在しない場合は memes を作成します。
        </p>

    </up-section>

    <up-section title="Arguments">
        <arguments-list args={this.args}></arguments-list>
    </up-section>

    <up-section title="Values">
        <p>
            memes
        </p>
    </up-section>

    <up-section title="Conditions">
    </up-section>

    <up-section title="See also">
        <p>tx-add-lmemes</p>
        <p>%get-memes</p>
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
             name: 'ensure',
             type: 'boolean',
             description: ''
         }
     ];
    </script>
</get-memes>

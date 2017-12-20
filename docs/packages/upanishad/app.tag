<app>
    <up-header title="Package: UPANISHAD"></up-header>
    <up-section title="Description">
        <p>
            このパッケージについてはまだ整理が仕切れていない印象。<br/>
            やはり POOL に対するオペレータなどは upanishad.pool パッケージを作ってそこで管理すべきではないか。<br/>
            雑音が多すぎる印象。<br/>
        </p>
        <p>
            基本ユーザーは meme, index, transaction の操作が出来れば良いはず。<br/>
            pool に対する直接的な操作は通常づかいではしないはず。<br/>
            あ、でも pool の snapshot, restore は必要か。<br/>
            まぁそんなもんかな。
        </p>
    </up-section>

    <up-section title="Operators">
        <operater-over-view></operater-over-view>
    </up-section>

    <up-section title="meme">
    </up-section>

    <up-section title="index">
    </up-section>

    <up-section title="Symbols">
        <symbol-list data={STORE.state().symbols}
                     package="upanishad">
        </symbol-list>
    </up-section>

    <up-footer></up-footer>
    <up-nav></up-nav>
</app>

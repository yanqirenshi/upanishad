<app>
    <up-header title="Package: {STORE.state().package.name.toUpperCase();}"></up-header>

    <up-section title="Description">
    </up-section>

    <up-section title="Symbols">
        <sbutitle>エクスポートされているシンボルの一覧です。</sbutitle>
        <p>全てのシンボルの一覧は <a href="./symbols">./symbols</a> を参照してください。</p>
        <symbol-list data={STORE.get('symbols')}
                     package={STORE.get('package.name')}
                     export={true}>
        </symbol-list>
    </up-section>

    <up-footer></up-footer>
    <up-nav></up-nav>
</app>

<app>
    <up-header title="Symbols @{STORE.state().package.name.toUpperCase()}">
        <subtitle>
            <a href="..">{STORE.state().package.name.toUpperCase()}</a> のシンボルの一覧
        </subtitle>
    </up-header>

    <up-section title="Symbols">
        <symbol-list data={STORE.state().symbols}
                     package={STORE.state().package.name}>
        </symbol-list>
    </up-section>

    <up-footer></up-footer>
    <up-nav></up-nav>
</app>

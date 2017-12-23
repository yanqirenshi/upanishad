<app>
    <up-header title="Package: {STORE.state().package.name.toUpperCase();}"></up-header>

    <up-section title="Description">
    </up-section>

    <up-section title="Symbols">
        <symbol-list data={STORE.state().symbols}
                     package={STORE.state().package.name}>
        </symbol-list>
    </up-section>

    <up-footer></up-footer>
    <up-nav></up-nav>
</app>

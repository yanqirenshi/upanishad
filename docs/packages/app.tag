<app>
    <up-header title="Packages"></up-header>
    <up-section title="Description"></up-section>

    <packages config={STORE.state().config}
              data={STORE.state().packages}></packages>

    <up-footer></up-footer>
    <up-nav></up-nav>
</app>

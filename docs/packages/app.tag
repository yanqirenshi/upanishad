<app>
    <up-header title="Packages"></up-header>
    <up-section title="Description"></up-section>

    <packages config={STORE.state().config}
              data={STORE.state().packages}></packages>

    <up-section title="Dependencies">
        <pre>
            <code>
 +-------+                 +------+
 | meme  |---------------->| pool |
 |       |    +-------+    |      |
 |       |--->| memes |--->|      |
 |       |    |       |    |      |
 +-------+    |       |    |      |
 +-------+    |       |    |      |
 | index |--->|       |    |      |
 |       |    +-------+    |      |
 |       |---------------->|      |
 +-------+                 +------+
        </code>
        </pre>
    </up-section>

    <up-footer></up-footer>
    <up-nav></up-nav>
</app>

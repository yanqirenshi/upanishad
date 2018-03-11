<page-1>
    <section class="section">
        <div class="container">
            <h1 class="title">Packages</h1>
            <h2 class="subtitle">
            </h2>
            <div class="contents">
                <package-list data={state().packages}></package-list>
            </div>
        </div>

        <section class="section">
            <div class="container">
                <h1 class="title is-3">UPANISHAD</h1>
                <div class="contents">
                </div>
            </div>
        </section>

        <section class="section">
            <div class="container">
                <h1 class="title is-3">UPANISHAD.POOL</h1>
                <div class="contents">
                </div>
            </div>
        </section>

        <section class="section">
            <div class="container">
                <h1 class="title is-3">UPANISHAD.MEME</h1>
                <div class="contents">
                </div>
            </div>
        </section>

        <section class="section">
            <div class="container">
                <h1 class="title is-3">UPANISHAD.MEMES</h1>
                <div class="contents">
                </div>
            </div>
        </section>

        <section class="section">
            <div class="container">
                <h1 class="title is-3">UPANISHAD.INDEX</h1>
                <div class="contents">
                </div>
            </div>
        </section>

    </section>

    <script>
     this.state = function () { return STORE.state().pages['page-1']; };

     this.on('update', function () {
         console.log(this.state());
     }.bind(this));

     STORE.subscribe(function (action) {
         console.log(action.type=='LOADED-DATA-PAGE-1');
         if (!action.type=='LOADED-DATA-PAGE-1') return;

         this.update();
     }.bind(this));
    </script>
</page-1>

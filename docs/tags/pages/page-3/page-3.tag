<page-3>
    <section class="section">
        <div class="container">
            <h1 class="title">Symbols</h1>
            <h2 class="subtitle">
            </h2>
            <div class="contents">
                <symbol-list data={state().symbols}></symbol-list>
            </div>
        </div>
    </section>

    <script>
     this.state = function () { return STORE.state().pages['page-3']; };

     STORE.subscribe(function (action) {
         if (!action.type=='LOADED-DATA-PAGE-3') return;
         this.update();
     }.bind(this));
    </script>
</page-3>

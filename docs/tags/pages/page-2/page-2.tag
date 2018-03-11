<page-2>
    <section class="section">
        <div class="container">
            <h1 class="title">Classes</h1>
            <h2 class="subtitle">
            </h2>

            <div class="contents">
                <class-list data={state().classes}></class-list>
            </div>
        </div>
    </section>

    <script>
     this.state = function () { return STORE.state().pages['page-2']; };

     STORE.subscribe(function (action) {
         if (!action.type=='LOADED-DATA-PAGE-2') return;

         this.update();
     }.bind(this));
    </script>
</page-2>

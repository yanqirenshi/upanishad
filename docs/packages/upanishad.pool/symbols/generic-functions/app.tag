<app>
    <home style={STYLE.display(STORE.get('tags.home'))}></home>
    <get-index style={STYLE.display(STORE.get('tags.get-index'))}></add-meme>

    <up-footer></up-footer>
    <up-nav></up-nav>

    <script>
     STORE.subscribe(function (action) {
         if (action.type=='MOVE-PAGE') {
             this.update();
             return;
         }
     }.bind(this));
    </script>
</app>

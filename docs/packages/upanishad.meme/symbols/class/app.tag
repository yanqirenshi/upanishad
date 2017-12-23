<app>
    <home style={STYLE.display(STORE.get('tags.home'))}></home>
    <memes style={STYLE.display(STORE.get('tags.memes'))}></memes>

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

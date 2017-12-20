<app>
    <home style={STYLE.display(STORE.get('tags.home'))}></home>
    <add-meme style={STYLE.display(STORE.get('tags.add-meme'))}></add-meme>
    <make-memes style={STYLE.display(STORE.get('tags.make-memes'))}></make-memes>
    <remove-meme style={STYLE.display(STORE.get('tags.remove-meme'))}></remove-meme>

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

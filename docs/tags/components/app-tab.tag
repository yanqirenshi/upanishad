<app-tab>
    <div class="tabs">
        <ul>
            <li each={page , code in opts.data}
                class="{page.display ? 'is-active' : ''}">
                <a code={code} onclick={onClickTab}>
                    {page.label}
                </a>
            </li>
        </ul>
    </div>

    <script>
     this.onClickTab = function (e) {
         STORE.dispatch(
             ACTIONS.movePage(e.target.getAttribute('code'))
         );
     };
    </script>
</app-tab>

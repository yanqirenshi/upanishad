<pool>
    <section class="section">
        <div class="container">
            <h1 class="title is-2">Class: MEMES</h1>
            <p class="subtitle">meme を管理するクラスです。</p>

            <h3 class="title is-3" style="margin-top:8px;">package</h3>
            <a href="../package">memes</a>

        </div>
    </section>

    <section class="section">
        <div class="container">
            <h1 class="title is-2">Slots</h1>
            <!-- <p class="subtitle"></p> -->
            <table class="table is-bordered is-striped is-narrow is-fullwidth">
                <thead>
                    <tr>
                        <th>name</th>
                        <th>type</th>
                        <th>accessor</th>
                        <th>initarg</th>
                        <th>initform</th>
                    </tr>
                </thead>
                <tbody>
                    <tr each="{POOL_CLASS.slots}">
                        <td><a href="#POOL-{name}">{name}</a></td>
                        <td>{type}</td>
                        <td>{accessor}</td>
                        <td>{initarg}</td>
                        <td>{initform}</td>
                    </tr>
                </tbody>
            </table>

            <section class="section article-item" id="">
                <div class="container">
                    <h1 class="title is-3">MEMES</h1>
                    <!-- <p class="subtitle"></p> -->
                    <pre>
                        <code>
+------------------------------+
| ht                           |
|==============================|
| package | +----------------+ |
|         | | ht             | |
|         | |================| |
|         | | class | object | |
|         | +----------------+ |
+------------------------------+
                        </code>
                    </pre>
                    <operators-table operators="{POOL_CLASS.operators}"
                                     targets="{['tx-add-memes', 'tx-remove-memes', 'get-memes']}">
                    </operators-table>

                    <h1 class="title is-4">MEME</h1>
                    <operators-table operators="{POOL_CLASS.operators}"
                                     targets="{['get-meme', 'find-meme',
                                                'x-change-meme-slots',
                                                'tx-create-meme', 'tx-delete-meme']}"></operators-table>
                </div>
            </section>

            <section class="section article-item" id="MEMES-">
                <div class="container">
                    <h1 class="title is-3">INDEXES</h1>
                    <!-- <p class="subtitle"></p> -->
                    <pre><code>
+------------------------------+
| ht                           |
|==============================|
| package | +----------------+ |
|         | | ht             | |
|         | |================| |
|         | | class | object | |
|         | +----------------+ |
+------------------------------+
                    </code></pre>
                    <operators-table operators="{POOL_CLASS.operators}"
                                     targets="{['get-index', 'tx-add-index', 'tx-remove-index']}"></operators-table>

                    <h1 class="title is-4">INDEX</h1>
                    <operators-table operators="{POOL_CLASS.operators}"
                                     targets="{['tx-add-meme-to-index',
                                                'tx-remove-meme-from-index']}"></operators-table>
                </div>
            </section>

            <h1 class="title is-3">V.1 廃止予定</h1>

            <section class="section article-item" id="">
                <div class="container">
                    <h1 class="title is-3">ROOT-OBJECTS</h1>
                    <!-- <p class="subtitle"></p> -->
                </div>
            </section>

            <section class="section article-item" id="">
                <div class="container">
                    <h1 class="title is-3">INDEX-OBJECTS</h1>
                    <!-- <p class="subtitle"></p> -->
                </div>
            </section>

        </div>
    </section>


    <section class="section">
        <div class="container">
            <h1 class="title is-2">Operators</h1>
            <!-- <p class="subtitle"></p> -->
            <operators-table operators="{POOL_CLASS.operators}"></operators-table>
        </div>
    </section>

    <style>
     dependencies > .section {
         padding-top: 11px;
         padding-bottom: 11px;
     }

     section .article-item {
         border: solid 1px #eee;
         border-radius: 3px;
         padding-top: 11px;
         margin-bottom: 22px;
     }
    </style>

    <script>
     this.ht = {
         memes: ['tx-add-memes', 'tx-remove-memes', 'get-memes']
     }
    </script>
</pool>

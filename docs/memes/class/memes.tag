<memes>
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
                    <tr>
                        <td>
                            <a href="#MEMES-meme-class">meme-class</a>
                        </td>
                        <td>class</td>
                        <td>meme-class</td>
                        <td>:meme-class</td>
                        <td>nil</td>
                    </tr>
                    <tr>
                        <td>
                            <a href="#MEMES-MEME-LIST">meme-list</a>
                        </td>
                        <td>list</td>
                        <td>meme-list</td>
                        <td>:meme-list</td>
                        <td>nil</td>
                    </tr>
                    <tr>
                        <td>
                            <a href="#MEMES-%ID-INDEX">%id-index</a>
                        </td>
                        <td>hash-table</td>
                        <td>%id-index</td>
                        <td>:%id-index</td>
                        <td>(make-hash-table)</td>
                    </tr>
                </tbody>
            </table>
            <section class="section article-item" id="MEMES-meme-class">
                <div class="container">
                    <h1 class="title is-3">MEME-CLASS</h1>
                    <!-- <p class="subtitle"></p> -->
                </div>
            </section>
            <section class="section article-item" id="MEMES-MEME-LIST">
                <div class="container">
                    <h1 class="title is-3">MEME-LIST</h1>
                    <!-- <p class="subtitle"></p> -->
                </div>
            </section>
            <section class="section article-item" id="MEMES-%ID-INDEX">
                <div class="container">
                    <h1 class="title is-3">%ID-INDEX</h1>
                    <!-- <p class="subtitle"></p> -->
                </div>
            </section>
        </div>
    </section>


    <section class="section">
        <div class="container">
            <h1 class="title is-2">Operators</h1>
            <!-- <p class="subtitle"></p> -->
            <table class="table is-bordered is-striped is-narrow is-fullwidth">
                <thead>
                    <tr>
                        <th>exp</th>
                        <th>type</th>
                        <th>symbol</th>
                        <th>args</th>
                        <th>description</th>
                    </tr>
                </thead>
                <tbody>
                    <tr>
                        <td>○</td>
                        <td>Function</td>
                        <td><a href="../function#get-meme">get-meme</a></td>
                        <td>memes &key %id slot value</td>
                        <td>memes から meme を取得する関数です。</td>
                    </tr>
                    <tr>
                        <td>○</td>
                        <td>Generic function</td>
                        <td><a href="../generic-function#add-meme">add-meme</a></td>
                        <td>memes meme</td>
                        <td>memes に meme を追加する総称関数です。</td>
                    </tr>
                    <tr>
                        <td>○</td>
                        <td>Generic function</td>
                        <td><a href="../generic-function#make-memes">make-memes</a></td>
                        <td>meme-class &key meme-list</td>
                        <td>memes インスタンスを作成する総称関数です。</td>
                    </tr>
                    <tr>
                        <td>○</td>
                        <td>Generic function</td>
                        <td><a href="../generic-function#remove-meme">remove-meme</a></td>
                        <td>memes meme</td>
                        <td>memes から meme を削除する総称関数です。</td>
                    </tr>
                </tbody>
            </table>
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
</memes>

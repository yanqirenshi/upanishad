<packages>
    <section class="section">
        <div class="container">
            <h1 class="title is-2">Packages</h1>
            <p class="subtitle">徐々に分解中</p>
            <table class="table is-bordered is-striped is-narrow is-fullwidth">
                <thead>
                    <tr>
                        <th>package</th>
                        <th>description</th>
                    </tr>
                </thead>

                <tbody>
                    <tr each={this.opts.data}>
                        <td><a href="/{this.opts.config.path.prefix}/packages/{name}">{name}</a></td>
                        <td>{description}</td>
                    </tr>
                </tbody>
            </table>
        </div>
    </section>

    <style>
     packages > .section {
         padding-top: 11px;
         padding-bottom: 11px;
     }
    </style>

    <script>
     console.log(this.opts);
    </script>
</packages>

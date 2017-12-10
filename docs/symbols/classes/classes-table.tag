<classes-table>
    <table class="table is-bordered is-striped is-narrow is-hoverable">
        <thead>
            <tr>
                <th>Symbol</th>
                <th>Description</th>
                <th>Package</th>
                <th>Parent</th>
            </tr>
        </thead>
        <hbody>
            <tr each="{this.opts.data}">
                <td><a href="#">{symbol}</a></td>
                <td>{description}</td>
                <td><a href="#">{this.parents2str(parents)}</a></td>
                <td><a href="#">{package}</a></td>
            </tr>
        </hbody>
    </table>

    <script>
     this.parents2str = function (parents) {
         var out = '';
         for (var i in parents)
             out += parents[i];
         return out;
     };
    </script>

</classes-table>

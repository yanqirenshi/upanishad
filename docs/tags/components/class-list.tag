<class-list>
    <table class="table">
        <thead>
            <th>PACKAGE</th>
            <th>NAME</th>
            <th>PARENTS</th>
            <th>DESCRIPTION</th>
        </thead>
        <tbody>
            <tr each={opts.data}>
                <td>{package}</td>
                <td>{symbol}</td>
                <td>{parentsStr(parents)}</td>
                <td>{description}</td>
            </tr>
        </tbody>
    </table>

    <script>
     this.parentsStr = function (parents) {
         let out = '';
         for (var i in parents)
             out += parents + ' ';
         return out;
     }.bind();
    </script>
</class-list>

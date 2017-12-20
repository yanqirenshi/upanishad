<operators-table>
    <table class="table is-bordered is-striped is-narrow">
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
            <tr each="{getOperators()}">
                <td>{export ? '○' : '--'}</td>
                <td style="white-space:nowrap;">{type}</td>
                <td style="white-space:nowrap;">{symbol}</td>
                <td>{args}</td>
                <td>{description}</td>
            </tr>
        </tbody>
    </table>

    <script>
     this.getOperators = function () {
         if (!this.opts.operators)
             return [];

         var operators = this.opts.operators;
         var targets = this.opts.targets;
         var out = [];

         if (targets && targets.length>0) {
             var ht = {}
             for (var i in targets)
                 ht[targets[i]]=true;

             for (var i in operators) {
                 var operator = operators[i];
                 if (ht[operator.symbol])
                     out.push(operator);
             }
         } else {
             out = operators;
         }
         console.log(out);
         return out;
     }
    </script>
</operators-table>

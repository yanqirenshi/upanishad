<symbol-list>
    <table class="table">
        <thead>
            <tr>
                <th>Type</th>
                <th>Name</th>
                <th>Description</th>
            </tr>
        </thead>
        <tbody>
            <tr each={this.filter(this.opts.data)}>
                <td>{this.type_str(type)}</td>
                <td><a href="{this.opts.link-head}#{name}">{name}</a></td>
                <td>{description}</td>
            </tr>
        </tbody>
    </table>

    <script>
     this.type_str = function (type) {
         if (type=='function')
             return 'Function';

         if (type=='generic-function')
             return 'Generic function';

         if (type=='class')
             return 'Class';

         if (type=='macro')
             return 'Macro';

         if (type=='condition')
             return 'Condition';

         return '???(' + type + ')';
     };

     this.need_p = function (symbol, conditions) {
         var package_ret = false;
         var type_ret = false;

         if (!conditions.package) {
             package_ret = true;
         } else {
             package_ret = (symbol.package == conditions.package);
         }
         if (!conditions.type) {
             type_ret = true;
         } else {
             type_ret = (symbol.type == conditions.type);
         }
         return package_ret && type_ret;
     };
     this.filter = function (symbols) {
         var out = [];
         var conditions = {
             package: this.opts.package,
             type: this.opts.type
         };
         for (var i in symbols)
             if (this.need_p(symbols[i], conditions))
                 out.push(symbols[i]);

         return out.sort(function (a, b) {
             return a.name < b.name ? -1 : 1;
         });
     }.bind(this);
    </script>
</symbol-list>

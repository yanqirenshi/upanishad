<operater-over-view>
    <table class="table is-bordered is-striped is-narrow is-hoverable">
        <thead>
            <tr>
                <th></th>
                <th each={column, i in this.columns}>{column}</th>
            </tr>
        </thead>
        <tbody>
            <tr each={contents, key in STORE.state().elements}>
                <td>{key}</td>
                <td each={v, k in contents}>{v ? "○" : "-"}</td>
            </tr>
        </tbody>
    </table>

    <script>
     this.columns = [
         'get',
         'find',
         'set',
         '(setf)',
         'add',
         'make',
         'create',
         'remove',
         'delete',
         'change',
         'reset'
     ];
    </script>
</operater-over-view>

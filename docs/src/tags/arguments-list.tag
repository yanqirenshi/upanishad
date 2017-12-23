<arguments-list>
    <table class="table is-bordered is-striped is-narrow is-hoverable">
        <thead>
            <tr>
                <th>name</th>
                <th>type</th>
                <th>description</th>
            </tr>
        </thead>
        <tbody>
            <tr each={this.opts.args}>
                <td>{name}</td>
                <td>{type}</td>
                <td>{description}</td>
            </tr>
        </tbody>
    </table>
</arguments-list>

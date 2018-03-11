<package-list>
    <table class="table">
        <thead>
            <th>NAME</th>
            <th>DESCRIPTION</th>
        </thead>
        <tbody>
            <tr each={opts.data}>
                <td>{name}</td>
                <td>{description}</td>
            </tr>
        </tbody>
    </table>
</package-list>

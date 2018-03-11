<symbol-list>
    <table class="table">
        <thead>
            <th>PACKAGE</th>
            <th>NAME</th>
            <th>TYPE</th>
            <th>DESCRIPTION</th>
        </thead>
        <tbody>
            <tr each={opts.data}>
                <td>{package}</td>
                <td>{name}</td>
                <td>{type}</td>
                <td>{description}</td>
            </tr>
        </tbody>
    </table>
</symbol-list>

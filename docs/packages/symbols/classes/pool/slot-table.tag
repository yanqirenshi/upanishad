<slot-table>
    <table class="table is-bordered is-striped is-narrow is-hoverable">
        <thead>
            <tr>
                <th>Symbol</th>
                <th>Description</th>
            </tr>
        </thead>
        <hbody>
            <tr each="{this.opts.data}">
                <td></td>
                <td>{description}</td>
            </tr>
        </hbody>
    </table>
</slot-table>

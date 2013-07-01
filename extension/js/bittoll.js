function makeDict(login, dict) {
    dict["username"] = login["username"]
    dict["time"] = (new Date().getTime() / 1000).toString()
    dict["sign"] = MD5(
        _.reduce(
            _.sortBy(
                _.pairs(dict),
                function(a) {return a[0]}
            ).concat([["secret", login["secret"]]]),
            function(memo, item) { return memo.concat(item[1]) },
            ""
        )
    )

    return dict
}

# How to Configure Ranger

In case you want to configure Ranger, the default configuration is a good place
to start. This configuration can be acquired by means of the
`--copy-config=all` option.

In order to populate `TARGETDIR` with the default configuration, run:

```sh
ranger --copy-config=all --confdir=TARGETDIR
```

> :bulb: You can run the command with `TARGETDIR` set to a path in `/tmp`
> (e.g.: `/tmp/default-ranger-conf`) in order to have a reference for your own
> configuration attempts.

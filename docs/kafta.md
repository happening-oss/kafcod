# kafta

Note that `kafka-topics` takes ~5 seconds just to *start* the JVM; if you'd prefer something written in Go (i.e.
faster):

```sh
cd /tmp     # it's got a README.md in it; avoid overwriting yours
wget https://github.com/electric-saw/kafta/releases/download/v0.1.8/kafta_Darwin_arm64.tar.gz
tar xf kafta_Darwin_arm64.tar.gz kafta        # but we'll only get the binary out anyway, just in case.
sudo mv kafta /usr/local/bin
```

```sh
kafta config set-context localhost \
    --server $(./docker/get-host-ip.sh):9092 \
    --sasl false \
    --tls false \
    --schema-registry ''
kafta config use-context localhost
```

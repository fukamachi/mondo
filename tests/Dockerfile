FROM fukamachi/sbcl

WORKDIR /app

RUN set -x; \
  apt-get update && apt-get -y install --no-install-recommends libreadline-dev && \
  rm -rf /var/lib/apt/lists*

RUN set -x; \
  mkdir -p "$HOME/.config/common-lisp/source-registry.conf.d/" && \
  echo '(:tree "/app")' >> "$HOME/.config/common-lisp/source-registry.conf.d/app.conf" && \
  ros install rove

ENTRYPOINT ["rove", "mondo.asd"]

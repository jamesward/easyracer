# JRuby + concurrent-ruby

Scenarios live under `src/main/ruby`; integration tests under `src/test/ruby`. HTTP races use **concurrent-ruby** and **Testcontainers Ruby** ([testcontainers-ruby](https://github.com/testcontainers/testcontainers-ruby)).

---

##  How to run in local

```bash
cd jruby-concurrent
jruby -S bundle install
jruby -S bundle exec rake test
```

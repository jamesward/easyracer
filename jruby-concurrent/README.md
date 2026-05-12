# JRuby + concurrent-ruby

Scenarios live under `src/main/ruby`; integration tests under `src/test/ruby`. HTTP races use **concurrent-ruby** and **Testcontainers Ruby** ([testcontainers-ruby](https://github.com/testcontainers/testcontainers-ruby)).

---

##  How to run in local

```bash
cd jruby-concurrent
jruby -S bundle install
jruby -S bundle exec rake test
```

## References

- https://www.ruby-lang.org/en/
- https://github.com/ruby-concurrency/concurrent-ruby
- https://guides.rubygems.org/gemfile/
- https://github.com/ruby/rake
- https://www.jruby.org/
- https://github.com/testcontainers/testcontainers-ruby

guard :shell do
  watch(%r{.*\.cabal$}) do
    `stack build && stack test --test-arguments=--color; date`
  end

  watch(%r{src/.*hs}) do
    `stack build && stack test --test-arguments=--color; date`
  end

  watch(%r{test/.*hs}) do
    `stack build && stack test --test-arguments=--color; date`
  end
end

guard :shell do
  watch(%r{.*\.cabal$}) do
    `cabal build && cabal test --show-details=always --test-option=--color; date`
  end

  watch(%r{src/.*hs}) do
    `cabal build && cabal test --show-details=always --test-option=--color; date`
  end

  watch(%r{test/.*hs}) do
    `cabal build && cabal test --show-details=always --test-option=--color; date`
  end

end

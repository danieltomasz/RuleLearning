function p = createPrimitiveSet(hs,pos)
  p = {{'na',0}}; % always have the null element

  for i = 1:length(hs.train_vocab)
    p = [p {{'isa',hs.train_vocab(i)}}];
  end
  
  for j = 3:length(hs.F)
    inds = 1:3;
    others = inds(inds~=pos);
    for k = others % 1:pos-1 for only sequentially later
      p = [p {{hs.F{j},k}}];
    end
  end

end

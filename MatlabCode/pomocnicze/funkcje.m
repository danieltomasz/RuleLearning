% find the maximum likelihood hypothesis for each cluster (because this
% helps you interpret what a cluster means)
[ll mlhs] = findMLHypotheses(c,train,hs,params,index_cache);
pr = computeCRP(c,params);

for i = 1:length(mlhs)
  fprintf('rule %d: %s %d %s %d %s %d\n',i,mlhs{i}{1},mlhs{i}{2},mlhs{i}{3},...
    mlhs{i}{4},mlhs{i}{5},mlhs{i}{6})
end

disp(['likelihood = ' num2str(ll,'%2.0f') ' / prior = ' num2str(pr,'%2.0f') ...
  ' / score = ' num2str(ll+pr,'%2.0f')]); 

fprintf('\n');


function hs = createHypothesisSpace(hs)

  % for each place find out what the primitives are
  for i = 1:3 
    ps{i} = createPrimitiveSet(hs,i);
  end
    
  % now iterate through and create the whole setup
  % this could be a recursive function but it's easier to read for the
  % finite case when it's written out.
  for i = 1:length(ps{3})
    h3{i} = ps{3}{i};
  end
  
  c = 1;
  for i = 1:length(ps{2})
    for j = 1:length(ps{3})
      h2{c} = [ps{2}{i} ps{3}{j}];
      c = c + 1;
    end
  end
  
  c = 1;
  for i = 1:length(ps{1})
    for j = 1:length(ps{2})
      for k = 1:length(ps{3})
        hs.hs{c} = [ps{1}{i} ps{2}{j} ps{3}{k}];
        c = c + 1;
      end
    end
  end    
end

% sub function to create the set of possible primitives for each slot in the
% rule
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

% cache whether each rule applies to each test string. (same as
% cacheCardinalities, more or less).

function hs = cacheTest(hs)

  % done if train and test are the same
  if length(hs.test_vocab) == length(hs.train_vocab) && ...
      all(hs.test_vocab == hs.train_vocab)
    hs.true_of_test = hs.true_of;
    hs.all_test_strings = hs.all_strings;
    return;
  end
  
  % get all strings in test vocab
  c = 1;
  for i = hs.test_vocab
    for j = hs.test_vocab
      for k = hs.test_vocab
        hs.all_test_strings{c} = [i j k];
        c = c + 1;
      end
    end
  end  
  
  % now for each rule, apply it to each string
  tic
  for i = 1:length(hs.hs)
    for j = 1:length(hs.all_test_strings)
      hs.true_of_test(i,j) = applyRuleToString(hs.hs{i},hs.all_test_strings{j});
    end
  end
  toc  
end


function hs = cacheCardinalities(hs)

  disp('caching cardinalities for a new experiment.')
  disp('warning: if you are running this for an experiment with a large vocabulary,')
  disp('e.g. Gomez (2002), this could take a *very* long time (on the order of several days)');
  disp('')
  
  % get all strings in train vocab
  c = 1;
  for i = hs.train_vocab
    for j = hs.train_vocab
      for k = hs.train_vocab
        all_strings{c} = [i j k];
        c = c + 1;
      end
    end
  end
  
  % now for each rule, apply it to each string
  tic
  disp('testing each rule against each string')
  for i = 1:length(hs.hs)
    fprintf('%d ',i);
    if mod(i,20)==0, fprintf('\n'); end;
    
    for j = 1:length(all_strings)
      true_of(i,j) = applyRuleToString(hs.hs{i},all_strings{j});
    end
  end
  toc
  
  % now consolidate logically consistent hypotheses
  % do all pairwise comparisons
  tic
  disp('removing logically consistent hypotheses')
  disp('this part can take as long as or longer than testing each rule against each string')
  i = 1;
  while i < size(true_of,1)
    fprintf('%d ',i);
    if mod(i,20)==0, fprintf('\n'); end;

    j = i + 1;
    while j < size(true_of,1)
      if all(true_of(i,:) == true_of(j,:))
        true_of(j,:) = [];
        hs.hs(j) = [];
%         disp('deleted');
      else
        j = j + 1;
      end                
    end
    i = i + 1;
  end
  toc
  
  hs.all_strings = all_strings;
  hs.true_of = sparse(true_of); % store this as a sparse matrix, takes up less space
  hs.cardinalities = sum(true_of,2);
  hs.probs = 1./hs.cardinalities;  
  hs.probs(isinf(hs.probs)) = 0;
  hs.log_probs = log(hs.probs);  
end

function ic = cacheItems(ts,cs,is,hs)

% consolidate all the strings if train and test are different
if length(hs.test_vocab) == length(hs.train_vocab) && ...
    hs.train_vocab(1) == hs.test_vocab(1)
  all_strings = hs.all_strings;
  true_of = hs.true_of;
else
  all_strings = [hs.all_strings hs.all_test_strings];
  true_of = [hs.true_of hs.true_of_test];  
end

items = [cs is];
for i = 1:length(ts)
%   ic.train(i) = find(cellfun(@(x) all(ts{i} == x),all_strings));
  ic.train(i) = findString(ts{i},all_strings);
end

for i = 1:length(items)
%   ic.items(i) = find(cellfun(@(x) all(items{i} == x),all_strings),1);
  ic.items(i) = findString(items{i},all_strings);
end

end

% for some reason this method is empirically faster than the cellfuns
% above, perhaps the error checking in cellfun?
function i = findString(s,as)

  for i = 1:length(as)
    if s(1) == as{i}(1) && s(2)==as{i}(2) && s(3)==as{i}(3)
      return;
    end
  end
end   

function [train index_cache] = addNoiseToTraining(hs,train,params,index_cache)

for i = 1:length(train)
  if rand > params.alpha
    ind = randi(length(hs.all_strings));
    train{i} = hs.all_strings{ind};
    index_cache.train(i) = ind;
    %find(cellfun(@(x) all(train{i} == x),hs.all_strings));
  end  
end



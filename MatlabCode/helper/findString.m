function i = findString(s,as)

  for i = 1:length(as)
    if s(1) == as{i}(1) && s(2)==as{i}(2) && s(3)==as{i}(3)
      return;
    end
  end
end  
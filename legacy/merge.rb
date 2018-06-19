def merge xs, ys
  result = xs.dup

  (ys - xs).each do |y|
    #puts "finding #{y}"
    yidx = ys.index(y)
    #puts "found at: #{yidx}"
    before_y = ys.take(yidx).reverse_each.find do |before_y|
      xs.find(before_y)
    end
    #puts "found before y: #{before_y}"

    xidx = result.index(before_y)
    #puts "xidx at: #{xidx}"
    #puts "current: #{result}"
    if xidx
      result.insert(xidx + 1, y)
    else
      after_y = ys.drop(yidx + 1).find do |after_y|
        xs.find(after_y)
      end

      xidx = result.index(after_y)
      result.insert(xidx || -1, y)
    end
  end

  result
end

def verify xs, ys
  xs & ys == ys
end

data = File.read('gacha.txt').lines.inject({}) do |result, line|
  name, *rest = line.chomp.split(/\t+/)
  result[name] = rest
  result
end

epic = data['F1.EPIC63+2']
plat = data['PLA.63+8']
uber = data['F0.UBER63+3']
new = merge(epic, plat)
p verify(new, epic)
p verify(new, plat)
new2 = merge(epic, uber)
p verify(new2, epic)
p verify(new2, uber)

puts "for real"
aa = data.reject{|k, _|k.start_with?('PAL')}.values
result = aa.inject(epic) do |result, set|
  merge(result, set)
end
data.each do |name, set|
  puts "#{name}: #{verify(result, set)}"
end

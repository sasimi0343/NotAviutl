
function PathClipA(paths, startP, endP)
	for k,v in pairs(paths) do
		local index_start, per_start = math.modf((startP / 100) * #v.paths)
		local index_end, per_end = math.modf((1 - (endP / 100)) * #v.paths)
		local i = 0
		local indexes = {}
		for _,v2 in pairs(v.paths) do
			if (index_start > i) then
				table.insert(indexes, i+1)
			elseif (index_end < i) then
				table.insert(indexes, i+1)
			else
				if (index_start == i) then
					local nex = v.paths[i+2]
					if (not (nex == nil)) then
						v2.x = ((nex.x - v2.x) * per_start) + v2.x
						v2.y = ((nex.y - v2.y) * per_start) + v2.y
					end
				end
				if (index_end-1 == i) then
					local nex = v.paths[i+2]
					if (not (nex == nil)) then
						nex.x = ((nex.x - v2.x) * per_end) + v2.x
						nex.y = ((nex.y - v2.y) * per_end) + v2.y
					end
				end
			end
			i = i + 1
		end
		i = 0
		for _,v2 in pairs(indexes) do
			table.remove(v.paths, v2 - i)
			i = i + 1
		end
	end
end


function PathClipB(paths, startP, endP)
	for k,v in pairs(paths) do
		local distsum = 0
		local normdistlist = {}
		local i = 0
		for _,v2 in pairs(v.paths) do
			if (i < #v.paths-1) then
				local nex = v.paths[i+2]
				local dist = math.sqrt(math.pow(nex.x - v2.x, 2) + math.pow(nex.y - v2.y, 2))
				distsum = distsum + dist
				table.insert(normdistlist, dist)
			end
			i = i + 1
		end
		
		local startN, endN = (startP / 100), (1 - (endP / 100))
		
		local index_start, per_start = 0,0
		local index_end, per_end = 0,0
		local detS,detE = false, false
		
		local prevnormsum = 0
		local normsum = 0
		for l=1,#normdistlist+1 do
			local isnotlast = l <= #normdistlist
			if (isnotlast) then
				normdistlist[l] = normdistlist[l] / distsum
			end
			
			if (not detS and (startN < normsum or (not isnotlast and startN == 1))) then
				index_start = l-2
				per_start = (startN-prevnormsum)/(normsum-prevnormsum)
				detS = true
			end
			if (not detE and (endN < normsum or (not isnotlast and endN == 1))) then
				index_end = l-1
				per_end = (endN-prevnormsum)/(normsum-prevnormsum)
				detE = true
			end
			
			if (detE and detS) then
				break
			end
			
			if (isnotlast) then
				prevnormsum = normsum
				normsum = normsum + normdistlist[l]
			end
		end
		
		--print(index_start)
		
		local indexes = {}
		i = 0
		for _,v2 in pairs(v.paths) do
			if (index_start > i) then
				table.insert(indexes, i+1)
			elseif (index_end < i) then
				table.insert(indexes, i+1)
			else
				if (index_start == i) then
					local nex = v.paths[i+2]
					if (not (nex == nil)) then
						v2.x = ((nex.x - v2.x) * per_start) + v2.x
						v2.y = ((nex.y - v2.y) * per_start) + v2.y
					end
				end
				if (index_end-1 == i) then
					local nex = v.paths[i+2]
					if (not (nex == nil)) then
						nex.x = ((nex.x - v2.x) * per_end) + v2.x
						nex.y = ((nex.y - v2.y) * per_end) + v2.y
					end
				end
			end
			i = i + 1
		end
		i = 0
		for _,v2 in pairs(indexes) do
			table.remove(v.paths, v2 - i)
			i = i + 1
		end
	end
end

function CalcProportional(v, position)
	local distsum = 0
	local normdistlist = {}
	local i = 0
	for _,v2 in pairs(v.paths) do
		if (i < #v.paths-1) then
			local nex = v.paths[i+2]
			local dist = math.sqrt(math.pow(nex.x - v2.x, 2) + math.pow(nex.y - v2.y, 2))
			distsum = distsum + dist
			table.insert(normdistlist, dist)
		end
		i = i + 1
	end
	
	local startN = position
	
	local index_start, per_start = 0, 0
	local detS,detE = false, false
	
	local prevnormsum = 0
	local normsum = 0
	for l=1,#normdistlist+1 do
		local isnotlast = l <= #normdistlist
		if (isnotlast) then
			normdistlist[l] = normdistlist[l] / distsum
		end
		
		if (not detS and (startN < normsum or (not isnotlast and startN == 1))) then
			index_start = l-2
			per_start = (startN-prevnormsum)/(normsum-prevnormsum)
			break
		end
		
		if (isnotlast) then
			prevnormsum = normsum
			normsum = normsum + normdistlist[l]
		end
	end
	
	return index_start, per_start
end

return {
PathClipA = PathClipA,
PathClipB = PathClipB,
CalcProportional = CalcProportional
}

na_currentotogedata = nil

if (na_otogedata_cache == nil) then
	na_otogedata_cache = {}
end

function readOtoge(file, bpm, measure)
	if (not (na_otogedata_cache[file] == nil)) then
		na_currentotogedata = na_otogedata_cache[file]
		return na_otogedata_cache[file]
	end
	local fh = io.open(file)
	if (not (fh == nil)) then
		local str = fh:read("*a")
		fh:close()
		local otogedata = {notes = {}}
		local currentmeasure = {}
		local holdnotetimingstart = 0
		local isholding = false
		local measureindex = 0
		local measuresize = (60 / bpm) * measure
		for i=1,#str do
			local char = string.sub(str, i, i)
			if (char == "1") then
				table.insert(currentmeasure, 1)
			elseif (char == "2") then
				table.insert(currentmeasure, 2)
			elseif (char == "4") then
				table.insert(currentmeasure, 4)
			elseif (char == ",") then
				for l=1,#currentmeasure do
					local timing = (measuresize * ((l-1)/#currentmeasure)) + (measureindex * measuresize)
					if (currentmeasure[l] == 0) then
					elseif (currentmeasure[l] == 1) then
						table.insert(otogedata.notes, {type = 1, timing = timing})
					elseif (currentmeasure[l] == 2) then
						holdnotetimingstart = timing
						isholding = true
					elseif (currentmeasure[l] == 4) then
						if (isholding) then
							table.insert(otogedata.notes, {type = 2, timing = holdnotetimingstart, endtiming = timing})
							holdnotetimingstart = 0
							isholding = false
						end
					end
				end
				currentmeasure = {}
				measureindex = measureindex + 1
			else
				table.insert(currentmeasure, 0)
			end
		end
		na_otogedata_cache[file] = otogedata
		na_currentotogedata = otogedata
		return otogedata
	end
	return nil
end

return readOtoge
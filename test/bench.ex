without type_check 
 
integer left_edge, right_edge, top_edge, bottom_edge, max_iter, x_step,
    y_step, y0, x0, x, y, i, x_x, y_y, temp, the_char, accum, count
--atom t

--t = time()

accum = 0
count = 0
while count < 1545 do
    left_edge   = -420
    right_edge  =  300
    top_edge    =  300
    bottom_edge = -300
    x_step      =  7
    y_step      =  15

    max_iter    =  200

    y0 = top_edge
    while y0 > bottom_edge do
        x0 = left_edge
        while x0 < right_edge do
            y = 0
            x = 0
            the_char = 32
            x_x = 0
            y_y = 0
            i = 0
            while i < max_iter and x_x + y_y <= 800 do
                x_x = floor((x * x) / 200)
                y_y = floor((y * y) / 200)
                if x_x + y_y > 800 then
                    the_char = 48 + i
                    if i > 9 then
                        the_char = 64
                    end if
                else
                    temp = x_x - y_y + x0
                    if (x < 0 and y > 0) or (x > 0 and y < 0) then
                        y = (-1 * floor((-1 * x * y) / 100)) + y0
                    else
                        y = floor(x * y / 100) + y0
                    end if
                    x = temp
                end if

                i = i + 1
            end while
            accum = accum + the_char

            x0 = x0 + x_step
        end while

        y0 = y0 - y_step
    end while

    if remainder(count, 300) = 0 then
        --printf(1, "%d\n", accum)
        ? accum
    end if

    count = count + 1
end while

--printf(1, "%d\n\nCompleted in %d seconds\n\n", {accum, time() - t})

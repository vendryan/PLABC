class Test
    def initialize
        @x = 5
    end

    def two
        @y = 6
    end

    def three
        @y = @x + @y
    end
end

x = Test.new
x.three
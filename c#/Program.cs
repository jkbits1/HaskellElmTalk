using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace permCircles
{
    using WheelPos = IEnumerable<int>;

    class Program
    {
        static void Main (string[] args)
        {
            var first = new List<int> { 1, 2, 3, 4 };
            var second = new List<int> { 5, 6, 7, 8 };
            var third = new List<int> { 9, 10, 11, 12 };
            var answers = new List<int> { 15, 18, 21, 24 };

            WheelPos [] items = {};

            var secLoop = buildWheelLoop(items, second, second.Count() - 1);
            var thrLoop = buildWheelLoop(items, third, third.Count() - 1);
            var ansLoop = buildWheelLoop(items, answers, answers.Count() - 1);

            printWheel(secLoop);

            Console.WriteLine("done");
        }

        static void printWheel (IEnumerable<WheelPos> loop) {
            
            Console.WriteLine("Wheel loop");
            foreach (var pos in loop)
            {
                Console.WriteLine(pos);

                var posVals = pos.TakeWhile(x => true);

                Console.WriteLine("Wheel pos");
                foreach (var val in posVals)
                {
                    Console.Write(val);
                    Console.Write(" ");
                }
            }
        }

        static WheelPos turnWheel (WheelPos wheelPos, 
                                            int chunk) {
            IEnumerable<int> takePart = wheelPos.Take(chunk);
            IEnumerable<int> dropPart = wheelPos.Skip(chunk);

            WheelPos newPos = dropPart.Concat(takePart);

            return newPos;
        }

        static IEnumerable<WheelPos> buildWheelLoop 
                (IEnumerable<WheelPos> positions, WheelPos pos, int count) {
            IEnumerable<WheelPos> newPositions = null;

            if (count == 0)
            {
                Console.WriteLine("last pos of loop");

                WheelPos [] listFromPos = { pos };

                newPositions = positions.Concat(listFromPos);

                return newPositions;
            }

            Console.WriteLine("loop pos");

            WheelPos[] turn = { turnWheel(pos, count) };

            newPositions = positions.Concat(turn);                    

            IEnumerable<WheelPos> w = buildWheelLoop(newPositions, pos, count - 1); ;

            return w;
        }

    }

    
}

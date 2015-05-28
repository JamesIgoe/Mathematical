using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Libraries.Test
{
    [TestClass]
    public class StatTests
    {
        public StatTests()
        {

        }

        [TestMethod]
        public void StdDevTest()
        {
            double result = Stat.StdDev(new double[] { 1.1, 2.2, 3.3 });
            Assert.IsTrue(result > 0);
        }

        [TestMethod]
        public void VarianceTest()
        {
            double result = Stat.variance(new double[] { 1.1, 2.2, 3.3 });
            Assert.IsTrue(result > 0);
        }

        [TestMethod]
        public void SumOfSquaresTest()
        {
            double result = Stat.sumOfSquares(new double[] { 1.1, 2.2, 3.3 });
            Assert.IsTrue(result > 0);
        }

        [TestMethod]
        public void SquaredDifferenceArrayTest()
        {
            double[] result = Stat.SquaredDifferenceArray(new double[] { 1.1, 2.2, 3.3 });
            Assert.IsTrue(result.Length > 0);
        }
    }
}

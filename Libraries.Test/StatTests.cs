using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Libraries;

namespace Libraries.Test
{
    [TestClass]
    public class StatTests
    {
        [TestMethod]
        public void StdDevTest()
        {
            var result = Stat.StdDev(new double[] { 1.1, 2.2, 3.3 });
            Assert.IsTrue(result > 0);
        }

        [TestMethod]
        public void VarianceTest()
        {
            var result = Stat.Variance(new double[] { 1.1, 2.2, 3.3 });
            Assert.IsTrue(result > 0);
        }

        [TestMethod]
        public void SumOfSquaresTest()
        {
            var result = Stat.SumOfSquares(new double[] { 1.1, 2.2, 3.3 });
            Assert.IsTrue(result > 0);
        }

        [TestMethod]
        public void SquaredDifferenceArrayTest()
        {
            var result = Stat.SquaredDifferenceArray(new double[] { 1.1, 2.2, 3.3 });
            Assert.IsTrue(result.Length > 0);
        }
    }
}

using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using Microsoft.FSharp.Collections;
using System.Linq;

namespace Libraries.Test
{
    [TestClass]
    public class EulerLibTests
    {
        [TestMethod()]
        public void IsPrimeTest()
        {
            var resultPrimeTrue = EulerLib.IsPrime(7);
            Assert.IsTrue(resultPrimeTrue);

            var resultPrimeFalse = EulerLib.IsPrime(6);
            Assert.IsFalse(resultPrimeFalse);
        }

        [TestMethod()]
        public void GetPrimesTest()
        {
            var result = EulerLib.GetPrimes(100);
            Assert.IsNotNull(result);
        }

        [TestMethod()]
        public void FactorialTest()
        {
            var result = EulerLib.Factorial(3, 1);
            Assert.AreEqual(result, 6);
        }

        [TestMethod()]
        public void FactorialBigIntTest()
        {
            var result = EulerLib.FactorialBigInt(11);
            Assert.AreEqual(result, 39916800);
        }

        [TestMethod()]
        public void FibonacciArrayTest()
        {
            var result = EulerLib.FibonacciArray(1, 2, 99, FSharpList<int>.Empty);
            Assert.IsNotNull(result);
            Assert.AreEqual(result.Length, 8);
        }

        [TestMethod()]
        public void CollatzSequenceArrayTest()
        {
            var result = EulerLib.CollatzSequenceArray(13, 1, FSharpList<int>.Empty);
            Assert.IsNotNull(result);
            Assert.IsTrue(result.Length == 9);
        }

        [TestMethod()]
        public void CollatzSequenceCountTest()
        {
            var result = EulerLib.CollatzSequenceCount(13, 1);
            Assert.IsTrue(result == 10);
        }

        [TestMethod()]
        public void RandomArrayTest()
        {
            var result = EulerLib.RandomArray(30);
            Assert.IsTrue(result.Length == 30);
        }

    }
}

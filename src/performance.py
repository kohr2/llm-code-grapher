"""
Enhanced performance monitoring with retry logic
"""

import time
import psutil
import logging
from typing import Dict, List, Optional
from dataclasses import dataclass
from contextlib import contextmanager

@dataclass
class PerformanceMetrics:
    """Performance metrics container"""
    processing_time: float
    memory_peak: int
    memory_average: int
    cpu_percent: float
    retry_count: int

class PerformanceMonitor:
    """Enhanced performance monitoring with retry logic"""
    
    def __init__(self, max_retries: int = 3, retry_delay: float = 1.0):
        """
        Initialize performance monitor
        
        Args:
            max_retries: Maximum number of retries for failed operations
            retry_delay: Delay between retries in seconds
        """
        self.max_retries = max_retries
        self.retry_delay = retry_delay
        self.start_time = None
        self.end_time = None
        self.memory_samples = []
        self.cpu_samples = []
        self.retry_count = 0
        self.logger = logging.getLogger(__name__)
    
    @contextmanager
    def monitor_operation(self, operation_name: str):
        """Context manager for monitoring operations with retry logic"""
        self.start_monitoring()
        retry_count = 0
        
        while retry_count <= self.max_retries:
            try:
                yield self
                break
            except Exception as e:
                retry_count += 1
                self.retry_count = retry_count
                
                if retry_count <= self.max_retries:
                    self.logger.warning(
                        f"Operation '{operation_name}' failed (attempt {retry_count}), "
                        f"retrying in {self.retry_delay}s: {e}"
                    )
                    time.sleep(self.retry_delay)
                else:
                    self.logger.error(
                        f"Operation '{operation_name}' failed after {self.max_retries} retries: {e}"
                    )
                    raise
        
        self.stop_monitoring()
    
    def start_monitoring(self):
        """Start performance monitoring"""
        self.start_time = time.time()
        self.memory_samples = []
        self.cpu_samples = []
        self.retry_count = 0
        
        # Initial sample
        self._take_sample()
    
    def stop_monitoring(self):
        """Stop performance monitoring"""
        self.end_time = time.time()
        self._take_sample()
    
    def _take_sample(self):
        """Take a performance sample"""
        try:
            process = psutil.Process()
            self.memory_samples.append(process.memory_info().rss)
            self.cpu_samples.append(process.cpu_percent())
        except (psutil.NoSuchProcess, psutil.AccessDenied):
            # Handle cases where process info is not available
            self.memory_samples.append(0)
            self.cpu_samples.append(0.0)
    
    def get_metrics(self) -> PerformanceMetrics:
        """Get comprehensive performance metrics"""
        if not self.start_time or not self.end_time:
            raise ValueError("Monitoring not started or stopped")
        
        processing_time = self.end_time - self.start_time
        memory_peak = max(self.memory_samples) if self.memory_samples else 0
        memory_average = sum(self.memory_samples) / len(self.memory_samples) if self.memory_samples else 0
        cpu_average = sum(self.cpu_samples) / len(self.cpu_samples) if self.cpu_samples else 0.0
        
        return PerformanceMetrics(
            processing_time=processing_time,
            memory_peak=memory_peak,
            memory_average=int(memory_average),
            cpu_percent=cpu_average,
            retry_count=self.retry_count
        )
    
    def log_metrics(self, operation_name: str):
        """Log performance metrics"""
        metrics = self.get_metrics()
        self.logger.info(
            f"Performance metrics for '{operation_name}': "
            f"Time={metrics.processing_time:.2f}s, "
            f"Memory Peak={metrics.memory_peak / 1024 / 1024:.1f}MB, "
            f"Memory Avg={metrics.memory_average / 1024 / 1024:.1f}MB, "
            f"CPU={metrics.cpu_percent:.1f}%, "
            f"Retries={metrics.retry_count}"
        )

# Convenience functions
@contextmanager
def monitor_performance(operation_name: str, max_retries: int = 3):
    """Convenience function for performance monitoring"""
    monitor = PerformanceMonitor(max_retries=max_retries)
    with monitor.monitor_operation(operation_name) as m:
        yield m
    monitor.log_metrics(operation_name)